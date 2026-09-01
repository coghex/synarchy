{-# LANGUAGE Strict #-}
-- | Lua API for the structures debug builder (walls / floors / ceilings).
--   Textures + facemaps are loaded Lua-side via engine.loadTexture (which
--   returns the raw TextureHandle int); their handles are passed straight
--   into structure.place. Writes route through the WeSetStructure /
--   WeClearStructure edit path so the AUTHORITATIVE per-chunk 'lcStructures'
--   overlay (the same data rendering + persistence use) stays the single
--   source of truth and survives chunk eviction + save/load. (The old
--   'structureStoreRef', a SECOND authority rendering never read, is gone.)
--
--   Those edits apply asynchronously on the world thread, but the builder
--   places a piece and queries it within the same Lua call (floors→posts→
--   walls), so the read helpers consult the active world's per-world
--   'wsStructureStageRef' write-ahead cache first and fall back to
--   'lcStructures' — read-your-writes without a second authority. Per-world
--   so a placement can't leak across worlds (see 'wsStructureStageRef').
module Engine.Scripting.Lua.API.Structure
    ( structurePlaceFn
    , structureStageWatermarkFn
    , structureClearFn
    , structureClearAllFn
    , structureCountFn
    , structureFloorZAtFn
    , structureHasAtFn
    , structureGetAtFn
    , structureLoadedCountFn
    , structureUnresolvedPaletteIdsFn
    , structureSetPaletteHandleFn
    , structurePaletteCountFn
    , structureRegisterWallFamilyFn
    , resolveStructurePage
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv, activeWorldPage, activeWorldState)
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Asset.Handle (TextureHandle(..))
import Structure.Types
import Structure.Facing (WallEdge(..), WallCaps, wallCapsFromCode)
import Structure.Palette (internPath, lookupPath, TexPalette(..))
import Structure.WallCatalog (WallArtEntry(..), registerWallFamily)
import World.Types
    (wsTilesRef, wsStructureStageRef, wmWorlds, WorldPageId(..), WorldState
    , pageWrapWorldSize)
import World.Chunk.Types (LoadedChunk(..))
import World.Tile.Types (WorldTileData(..), lookupChunk)
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Command.Types (WorldCommand(..))

-- | Resolve which world page a structure op targets: a named page (any in
--   wmWorlds, even hidden / non-active) when a page-id string is given,
--   else the active world. Location stamping (#89) passes the page id so a
--   room authored on a hidden secondary page writes onto THAT page — not
--   whichever page happens to be active when its chunk loaded.
resolveStructurePage ∷ EngineEnv → Maybe Text → IO (Maybe (WorldPageId, WorldState))
resolveStructurePage env (Just pid) = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    pure $ (\ws → (WorldPageId pid, ws)) <$> lookup (WorldPageId pid) (wmWorlds mgr)
resolveStructurePage env Nothing = activeWorldPage env

-- | structure.place(gx, gy, slot, texHandle, faceHandle, z, texPath, facePath)
--   → bool. slot ∈ "floor"/"ceiling"/"post_n…w"/"wall_ne…sw".
--   texHandle/faceHandle = engine.loadTexture handles, recorded against the
--   piece's palette ids so the renderer can resolve it immediately.
--   texPath/facePath = the texture PATHS — interned into the save-level
--   palette + queued as a WeSetStructure edit (the persistent, per-chunk,
--   evict-survivable path, and the only path that actually places a piece).
--   Omit the paths and nothing is placed. z defaults to 0. Returns false (and
--   does nothing) when the paths are omitted, there is no active world, the
--   named page does not exist, or the target chunk isn't loaded — the last
--   because the world thread drops a structure edit for an unloaded chunk, so
--   staging one would be a phantom.
--
--   "Does nothing" is literal (#1675): every one of those rejections is decided
--   BEFORE the first mutation, so a false return leaves the texture palette,
--   the palette→handle map, the structure stage, the edit log and the world
--   queue all byte-identical. It has to be — the palette is a REQUIRED
--   persistent component whose unreferenced entries are deliberately never
--   pruned ('World.Save.Snapshot' checks only that every edit's ids exist), so
--   a path interned by a rejected call would ride into every later save. The
--   ids are needed only for the queued edit, so nothing before validation
--   depends on them; the order is validate → intern → stage → queue.
--
--   The chunk can still evict between that check and the world thread's own,
--   so the staged entry is tagged with this attempt's token and the command
--   carries it: a declined commit retracts exactly that entry (#1674).
structurePlaceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structurePlaceFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    texA  ← Lua.tointeger 4
    faceA ← Lua.tointeger 5
    zA    ← Lua.tointeger 6
    texPathA  ← Lua.tostring 7
    facePathA ← Lua.tostring 8
    pageA     ← Lua.tostring 9
    case (gxA, gyA, slotA, texA, faceA) of
        (Just gx, Just gy, Just slotBS, Just tex, Just face) →
            case slotFromText (TE.decodeUtf8Lenient slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    let z       = maybe 0 fromIntegral zA
                        gxi     = fromIntegral gx
                        gyi     = fromIntegral gy
                        slotTag = fromIntegral (fromEnum slot) ∷ Word8
                        handoff = toRenderHandoffCapability env
                    placed ← Lua.liftIO $
                        -- The per-chunk overlay (lcStructures), reached via the
                        -- WeSetStructure edit, is the SINGLE source of truth that
                        -- rendering + persistence read. VALIDATE the target, then
                        -- intern the PATHS → palette ids and queue the edit.
                        -- Omitting the paths places nothing — there is no separate
                        -- in-memory debug store.
                        --
                        -- #1675: validate BEFORE interning. The palette ids exist
                        -- only to ride the WeSetStructure edit, so nothing ahead of
                        -- the rejection branches needs them — and the palette is a
                        -- REQUIRED persistent component (texture-palette) whose
                        -- unreferenced entries are deliberately never pruned, so a
                        -- path interned by a call that then returns False would ride
                        -- into every later save. Ordering is validate → intern →
                        -- stage → queue so a False return mutates nothing at all.
                        case (texPathA, facePathA) of
                            (Just tp, Just fp) → do
                                mActive ← resolveStructurePage env (TE.decodeUtf8Lenient <$> pageA)
                                case mActive of
                                    Nothing → pure False
                                    Just (pageId, ws) → do
                                        -- #1175: place through the same
                                        -- canonical key hasAt/floorZAt read,
                                        -- so a staged piece and the edit that
                                        -- follows it name one physical tile.
                                        worldSize ← pageWrapWorldSize ws
                                        -- Only stage + queue when the target chunk
                                        -- is loaded: the world thread DROPS a
                                        -- WeSetStructure for an unloaded chunk
                                        -- (Edit.hs), so staging one regardless would
                                        -- leave a phantom that floorZAt/hasAt report
                                        -- as real though the world never changed
                                        -- (reachable stamping a room across a chunk
                                        -- boundary). Return false instead.
                                        td ← readIORef (wsTilesRef ws)
                                        let (coord, _, (dgx, dgy)) =
                                                canonicalTileFrame worldSize gxi gyi
                                            cgx = gxi + dgx
                                            cgy = gyi + dgy
                                        case lookupChunk coord td of
                                            Nothing → pure False
                                            Just _  → do
                                                -- The target is real: NOW intern the
                                                -- paths, which is the first mutation
                                                -- this call performs (#1675).
                                                texId  ← atomicModifyIORef' (rhTexPaletteRef handoff) $ \pal →
                                                    let (i, pal') = internPath (TE.decodeUtf8Lenient tp) pal
                                                    in (pal', i)
                                                faceId ← atomicModifyIORef' (rhTexPaletteRef handoff) $ \pal →
                                                    let (i, pal') = internPath (TE.decodeUtf8Lenient fp) pal
                                                    in (pal', i)
                                                -- record paletteId → handle so the
                                                -- renderer can resolve this piece
                                                -- immediately (the handle is already
                                                -- loaded — passed in by the builder)
                                                atomicModifyIORef' (rhTexPaletteHandlesRef handoff) $ \m →
                                                    ( HM.insert texId  (TextureHandle (fromIntegral tex))
                                                    $ HM.insert faceId (TextureHandle (fromIntegral face)) m
                                                    , () )
                                                -- staged for read-your-writes: the
                                                -- builder queries this tile within
                                                -- the same Lua call (floors→posts→
                                                -- walls) before the world thread has
                                                -- applied it. (See wsStructureStageRef.)
                                                -- The token this attempt takes rides
                                                -- the command, so a decline retracts
                                                -- THIS staged entry and no other
                                                -- (#1674).
                                                tok ← atomicModifyIORef' (wsStructureStageRef ws) $
                                                    stageStructurePlacement
                                                        (cgx, cgy, slotTag)
                                                        (StructurePieceData texId faceId z)
                                                Q.writeQueue (wsWorldQueue (toWorldSimCapability env))
                                                    (WorldSetStructure pageId cgx cgy
                                                                       slotTag texId faceId z tok)
                                                pure True
                            _ → pure False   -- no paths → nothing placed
                    Lua.pushboolean placed
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | structure.stageWatermark([pageId]) → integer — the token the NEXT
--   @structure.place@ on that page will take. Returns nil when the page
--   does not resolve.
--
--   Reading it either side of a synchronous run of placements yields
--   that run's 'StructureCommitWindow' (#2051): the half-open span of
--   the attempts it ACCEPTED, which
--   'World.Command.Types.WorldMarkLocationStamped' carries so the world
--   thread can withhold a completion marker when one of them was later
--   declined. Tokens come from the page's own counter and are handed out
--   only here on the Lua thread, so nothing can slip into that span.
--
--   The counter is a 'Word64' and Lua integers are signed 64-bit; a
--   session would have to stage 2^63 placements on one page for the
--   conversion to matter, which that counter cannot reach.
structureStageWatermarkFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureStageWatermarkFn env = do
    pageA ← Lua.tostring 1
    mNext ← Lua.liftIO $ do
        mPage ← resolveStructurePage env (TE.decodeUtf8Lenient <$> pageA)
        case mPage of
            Nothing      → pure Nothing
            Just (_, ws) → do
                st ← readIORef (wsStructureStageRef ws)
                let StructureStageToken n = ssNextToken st
                pure (Just n)
    case mNext of
        Nothing → Lua.pushnil
        Just n  → Lua.pushinteger (fromIntegral n)
    return 1

-- | structure.clear(gx, gy, slot) → bool — remove one piece from the
--   authoritative overlay via the WeClearStructure edit path (so it stays
--   cleared after eviction + save/load). Returns false if there is no active
--   world to queue against.
structureClearFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureClearFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    case (gxA, gyA, slotA) of
        (Just gx, Just gy, Just slotBS) →
            case slotFromText (TE.decodeUtf8Lenient slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    let gxi     = fromIntegral gx
                        gyi     = fromIntegral gy
                        slotTag = fromIntegral (fromEnum slot) ∷ Word8
                    ok ← Lua.liftIO $ do
                        mActive ← activeWorldPage env
                        case mActive of
                            Just (pageId, ws) → do
                                -- #1175: clear the SAME canonical key that
                                -- structure.place staged and hasAt reads, or
                                -- an aliased clear leaves the piece behind.
                                worldSize ← pageWrapWorldSize ws
                                let (cgx, cgy) = canonicalTile worldSize gxi gyi
                                -- drop any staged add (no-op if not staged) so a
                                -- re-query doesn't surface the just-cleared piece
                                atomicModifyIORef' (wsStructureStageRef ws) $ \st →
                                    (clearStagedKey (cgx, cgy, slotTag) st, ())
                                Q.writeQueue (wsWorldQueue (toWorldSimCapability env))
                                    (WorldClearStructure pageId cgx cgy slotTag)
                                pure True
                            Nothing → pure False
                    Lua.pushboolean ok
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | structure.clearAll() — wipe every structure piece in the active world
--   (live overlays + persisted edits) via the WorldClearAllStructures command,
--   and drop the read-your-writes staging cache.
structureClearAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureClearAllFn env = do
    Lua.liftIO $ do
        mActive ← activeWorldPage env
        case mActive of
            Just (pageId, ws) → do
                -- entries only: the attempt counter stays where it is, so a
                -- token retired by this wipe is never reissued and a decline
                -- for it cannot match a placement staged after it (#1674).
                atomicModifyIORef' (wsStructureStageRef ws) $ \st →
                    (clearStagedAll st, ())
                Q.writeQueue (wsWorldQueue (toWorldSimCapability env))
                    (WorldClearAllStructures pageId)
            Nothing → pure ()
    return 0

-- | structure.count() → int — distinct structure pieces in the active world:
--   the authoritative per-chunk overlay (lcStructures across loaded chunks)
--   unioned with the read-your-writes staging cache (just-placed pieces the
--   world thread hasn't applied yet). Agrees with floorZAt/hasAt.
structureCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureCountFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    n ← case mWs of
        Just ws → Lua.liftIO $ do
            st ← readIORef (wsStructureStageRef ws)
            td ← readIORef (wsTilesRef ws)
            let lcMap = HM.unions [ lcStructures lc | lc ← HM.elems (wtdChunks td) ]
            pure $ HM.size (HM.union (stagedPieces st) lcMap)
        Nothing → pure 0
    Lua.pushinteger (fromIntegral n)
    return 1

-- | structure.loadedCount() → int — pieces in the PERSISTENT per-chunk overlay
--   (lcStructures) across all loaded chunks of the active world. Verifies the
--   WeSetStructure edit path: nonzero after placement, and again after a
--   save/load (replayed from sdEdits).
structureLoadedCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureLoadedCountFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Just ws → do
            td ← Lua.liftIO $ readIORef (wsTilesRef ws)
            let n = sum [ HM.size (lcStructures lc)
                        | lc ← HM.elems (wtdChunks td) ]
            Lua.pushinteger (fromIntegral n)
            return 1
        Nothing → Lua.pushinteger 0 >> return 1

-- | structure.paletteCount() → int — number of texture paths in the palette
--   (debug probe for save/restore of the palette).
structurePaletteCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structurePaletteCountFn env = do
    pal ← Lua.liftIO $ readIORef (rhTexPaletteRef (toRenderHandoffCapability env))
    Lua.pushinteger (fromIntegral (HM.size (tpPathToId pal)))
    return 1

-- | structure.unresolvedPaletteIds() → { {id=, path=}, ... } — palette ids
--   that have no runtime texture handle yet (e.g. right after a load, which
--   clears the session-local handle map). The Lua resolve tick loadTexture's
--   each path and feeds it back via setPaletteHandle. Empty once all resolved.
structureUnresolvedPaletteIdsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureUnresolvedPaletteIdsFn env = do
    let handoff = toRenderHandoffCapability env
    pal     ← Lua.liftIO $ readIORef (rhTexPaletteRef handoff)
    handles ← Lua.liftIO $ readIORef (rhTexPaletteHandlesRef handoff)
    let unresolved = [ (i, p) | (i, p) ← HM.toList (tpIdToPath pal)
                              , not (HM.member i handles) ]
    Lua.newtable
    forM_ (zip [1 ..] unresolved) $ \(n, (i, p)) → do
        Lua.newtable
        Lua.pushinteger (fromIntegral i)
        Lua.setfield (-2) "id"
        Lua.pushstring (TE.encodeUtf8 p)
        Lua.setfield (-2) "path"
        Lua.rawseti (-2) n
    return 1

-- | structure.setPaletteHandle(id, texHandle) — record a resolved palette id →
--   runtime handle (the renderer then resolves pieces using that id).
structureSetPaletteHandleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureSetPaletteHandleFn env = do
    idA ← Lua.tointeger 1
    hA  ← Lua.tointeger 2
    case (idA, hA) of
        (Just i, Just h) → Lua.liftIO $
            atomicModifyIORef' (rhTexPaletteHandlesRef (toRenderHandoffCapability env)) $ \m →
                (HM.insert (fromIntegral i) (TextureHandle (fromIntegral h)) m, ())
        _ → pure ()
    return 0

-- | structure.registerWallFamily(entries) → bool — declare ONE structure
--   pack variant's complete directional wall art (#1712), so the renderer
--   can draw a wall with the sprite its edge occupies once the camera has
--   rotated. @entries@ is a dense array of
--   @{dir = "ne"|"nw"|"se"|"sw", cap = "00"|"10"|"01"|"11" or nil,
--   path = "...", handle = engine.loadTexture(path), owned = true|false}@
--   — the sprite for each direction (no @cap@) plus that direction's four
--   cap facemaps.
--
--   @owned@ is MANDATORY and says whether this family DECLARES the path or
--   merely inherits it from the pack's default art: a variant may override
--   any subset of the wall art, and claiming an inherited path would
--   rotate a DEFAULT wall into the variant's sprite (see
--   "Structure.WallCatalog"). It is required rather than defaulted so a
--   caller cannot reintroduce that silently.
--
--   All twenty must be present and well-formed: a partial family would
--   rotate some of a wall's directions and not others, so an incomplete or
--   malformed call returns false and registers NOTHING. Registration is
--   keyed by PATH, not by palette id, so it survives the wholesale palette
--   replacement a load performs and never needs redoing; re-registering a
--   variant is an idempotent no-op. Nothing here places a piece or touches
--   the palette — the catalogue is pure art metadata, read only at render
--   time.
structureRegisterWallFamilyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureRegisterWallFamilyFn env = do
    isT ← Lua.istable 1
    ok ← if not isT then pure False else do
        n ← Lua.rawlen 1
        mEntries ← readEntries (fromIntegral n)
        case mEntries of
            Nothing → pure False
            Just entries → Lua.liftIO $
                atomicModifyIORef' (rhStructureWallCatalogRef (toRenderHandoffCapability env)) $
                    \cat → case registerWallFamily entries cat of
                        Just cat' → (cat', True)
                        Nothing   → (cat, False)
    Lua.pushboolean ok
    return 1
  where
    -- Every entry must parse: a family assembled from the ones that
    -- happened to be well-formed is exactly the partial family the
    -- all-or-nothing rule exists to refuse.
    readEntries ∷ Int → Lua.LuaE Lua.Exception (Maybe [WallArtEntry])
    readEntries n = go 1 []
      where
        go i acc
            | i > n = pure (Just (reverse acc))
            | otherwise = do
                ty ← Lua.rawgeti 1 (fromIntegral i)
                mE ← if ty ≢ Lua.TypeTable then pure Nothing else readEntry
                Lua.pop 1
                case mE of
                    Nothing → pure Nothing
                    Just e  → go (i + 1) (e : acc)

    readEntry ∷ Lua.LuaE Lua.Exception (Maybe WallArtEntry)
    readEntry = do
        mDir   ← strField "dir"
        mPath  ← strField "path"
        mCap   ← strField "cap"
        _      ← Lua.getfield (-1) "handle"
        mH     ← Lua.tointeger (-1)
        Lua.pop 1
        ownTy  ← Lua.getfield (-1) "owned"
        owned  ← Lua.toboolean (-1)
        Lua.pop 1
        pure $ do
            dir  ← mDir ⌦ wallEdgeFromText
            path ← mPath
            caps ← readCaps mCap
            h    ← mH
            -- Absent or non-boolean `owned` is a malformed entry, never a
            -- silent default: see this function's header.
            guard (ownTy ≡ Lua.TypeBoolean)
            pure (WallArtEntry dir caps path (TextureHandle (fromIntegral h)) owned)

    -- An ABSENT cap names the direction's sprite; a PRESENT one must be a
    -- valid "<left><right>" suffix, never silently treated as absent.
    readCaps ∷ Maybe Text → Maybe (Maybe WallCaps)
    readCaps Nothing  = Just Nothing
    readCaps (Just c) = Just <$> wallCapsFromCode c

    strField ∷ Lua.Name → Lua.LuaE Lua.Exception (Maybe Text)
    strField name = do
        ty ← Lua.getfield (-1) name
        v  ← if ty ≢ Lua.TypeString then pure Nothing
             else fmap (fmap TE.decodeUtf8Lenient) (Lua.tostring (-1))
        Lua.pop 1
        pure v

    wallEdgeFromText ∷ Text → Maybe WallEdge
    wallEdgeFromText t = case t of
        "ne" → Just WallNE
        "nw" → Just WallNW
        "se" → Just WallSE
        "sw" → Just WallSW
        _    → Nothing

-- | structure.floorZAt(gx, gy) → int|nil — the z of the FLOOR at this tile,
--   or nil if there is none. Posts are only placeable where a floor exists,
--   and take the floor's z (so they render on the floor, not the terrain).
--   Reads the staging cache then the authoritative per-chunk overlay (see
--   'lookupStructure'), so it sees a floor placed earlier in the same Lua
--   call AND agrees with what is rendered/persisted after a save/load replay.
structureFloorZAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureFloorZAtFn env = do
    gxA ← Lua.tointeger 1
    gyA ← Lua.tointeger 2
    pageA ← Lua.tostring 3
    case (gxA, gyA) of
        (Just gx, Just gy) → do
            mSpd ← Lua.liftIO $
                lookupStructure env (TE.decodeUtf8Lenient <$> pageA)
                                (fromIntegral gx) (fromIntegral gy) SFloor
            case mSpd of
                Just spd → Lua.pushinteger (fromIntegral (spdGridZ spd)) >> return 1
                Nothing  → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | structure.hasAt(gx, gy, slot) → bool — is there a piece at this (tile,
--   slot)? Used by the wall builder to test for a corner post at a node.
--   Staging cache then authoritative overlay (see 'structureFloorZAtFn').
structureHasAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureHasAtFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    pageA ← Lua.tostring 4
    case (gxA, gyA, slotA) of
        (Just gx, Just gy, Just slotBS) →
            case slotFromText (TE.decodeUtf8Lenient slotBS) of
                Nothing → Lua.pushboolean False >> return 1
                Just slot → do
                    mSpd ← Lua.liftIO $
                        lookupStructure env (TE.decodeUtf8Lenient <$> pageA)
                                        (fromIntegral gx) (fromIntegral gy) slot
                    Lua.pushboolean (maybe False (const True) mSpd)
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | structure.getAt(gx, gy, slot[, page]) → {z=, tex=, face=}|nil — the piece
--   at this (tile, slot) with its texture/facemap PATHS resolved from the
--   palette. The persisted texture identity of a piece IS its palette path
--   (variant art included, #91), so this is how a headless test verifies a
--   damaged wall is still damaged after a save/load. Same staging-then-
--   authoritative lookup as hasAt.
structureGetAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
structureGetAtFn env = do
    gxA   ← Lua.tointeger 1
    gyA   ← Lua.tointeger 2
    slotA ← Lua.tostring 3
    pageA ← Lua.tostring 4
    case (gxA, gyA, slotA) of
        (Just gx, Just gy, Just slotBS) →
            case slotFromText (TE.decodeUtf8Lenient slotBS) of
                Nothing → Lua.pushnil >> return 1
                Just slot → do
                    mSpd ← Lua.liftIO $
                        lookupStructure env (TE.decodeUtf8Lenient <$> pageA)
                                        (fromIntegral gx) (fromIntegral gy) slot
                    case mSpd of
                        Nothing → Lua.pushnil >> return 1
                        Just spd → do
                            pal ← Lua.liftIO $ readIORef
                                    (rhTexPaletteRef (toRenderHandoffCapability env))
                            Lua.newtable
                            Lua.pushinteger (fromIntegral (spdGridZ spd))
                            Lua.setfield (-2) "z"
                            forM_ [ ("tex",  spdTexId spd)
                                  , ("face", spdFaceId spd) ] $ \(k, i) →
                                case lookupPath i pal of
                                    Just p  → do
                                        Lua.pushstring (TE.encodeUtf8 p)
                                        Lua.setfield (-2) k
                                    Nothing → pure ()
                            return 1
        _ → Lua.pushnil >> return 1

-- | Look up a structure piece for the builder. Consults the read-your-writes
--   staging cache first (a piece placed earlier in the SAME Lua call that the
--   world thread hasn't applied yet), then falls back to the AUTHORITATIVE
--   per-chunk overlay ('lcStructures') — the same data rendering and
--   persistence read, and what's left after a save/load replay (when the cache
--   is empty). Nothing if it's in neither: no staged add, and either no active
--   world, the chunk holding (gx,gy) isn't loaded, or no piece there.
--
--   #1175: the tile is canonicalised first, so @structure.hasAt@ /
--   @floorZAt@ / @getAt@ accept any u-alias — the same tolerance every
--   designation point verb has. Two callers depend on it: a build-tool
--   occupancy scan runs over ANCHOR-LOCAL alias tiles (a seam-side tile
--   would otherwise read free and the tool would record an accepted
--   outcome for a commit that creates no jobs), and a construct job whose
--   coord came out of a pre-#1175 save can still be an alias. Identity
--   away from the seam.
lookupStructure ∷ EngineEnv → Maybe Text → Int → Int → StructureSlot
                → IO (Maybe StructurePieceData)
lookupStructure env mPage rawGX rawGY slot = do
    mWs ← fmap snd <$> resolveStructurePage env mPage
    case mWs of
        Nothing → pure Nothing
        Just ws → do
            worldSize ← pageWrapWorldSize ws
            let (coord, _, (dgx, dgy)) = canonicalTileFrame worldSize rawGX rawGY
                key = ( rawGX + dgx, rawGY + dgy
                      , fromIntegral (fromEnum slot) ∷ Word8 )
            st ← readIORef (wsStructureStageRef ws)
            case HM.lookup key (ssEntries st) of
                Just staged → pure (Just (stgPiece staged))
                Nothing  → do
                    td ← readIORef (wsTilesRef ws)
                    pure $ lookupChunk coord td ⌦ HM.lookup key . lcStructures
